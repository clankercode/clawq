(** Tests for staged master-key rotation and resumable rewrap (P21.M2.E4.T007).
*)

module V = Github_user_token_vault
module S = Github_user_token_store
module MK = Github_user_token_master_key
module R = Github_user_token_rewrap

let () = Secret_store.test_iterations_override := Some 1

let aes_v1 =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-mk-v1" ()

let aes_v2 =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-mk-v2" ()

let aes_v3 =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-mk-OTHER" ()

let sample_tokens ~n =
  {
    S.access_token = Printf.sprintf "ghu_access_PLAIN_%d" n;
    refresh_token = Some (Printf.sprintf "ghr_refresh_PLAIN_%d" n);
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let fail_denial d = Alcotest.fail (R.string_of_denial d)
let fail_vdenial d = Alcotest.fail (V.string_of_denial d)

let account ?(principal_id = "prin_rw") ?(github_user_id = 100L) ?(app_id = 7)
    () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ())

(** Dual-key provider: Active = to (v2), available from (v1) as Backup_required.
*)
let rotation_keys ?(active = "mk-v2") ?(active_ver = 2) ?(active_aes = aes_v2)
    ?(from_id = "mk-v1") ?(from_ver = 1) ?(from_aes = aes_v1)
    ?(from_role = MK.Backup_required) () =
  let active_meta : MK.key_metadata =
    {
      key_id = active;
      key_version = active_ver;
      role = MK.Active;
      source_kind = MK.Env { var_name = "CLAWQ_GITHUB_VAULT_MASTER_KEY" };
    }
  in
  let from_meta : MK.key_metadata =
    {
      key_id = from_id;
      key_version = from_ver;
      role = from_role;
      source_kind = MK.Env { var_name = "CLAWQ_GITHUB_VAULT_MASTER_KEY_OLD" };
    }
  in
  let readiness =
    MK.Ready { active = active_meta; available = [ from_meta ] }
  in
  V.make_static_key_provider ~readiness
    ~keys:
      [
        { V.key_id = active; key_version = active_ver; aes_key = active_aes };
        { V.key_id = from_id; key_version = from_ver; aes_key = from_aes };
      ]
    ()

let single_keys ~key_id ~key_version ~aes =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key:aes ())

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  R.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_720_100_000.0

let seed_under_v1 ~db ~n =
  let keys = single_keys ~key_id:"mk-v1" ~key_version:1 ~aes:aes_v1 in
  let rec go i acc =
    if i > n then List.rev acc
    else
      let acct =
        account
          ~principal_id:(Printf.sprintf "prin_%d" i)
          ~github_user_id:(Int64.of_int (1000 + i))
          ()
      in
      let id = Printf.sprintf "ghvault_seed_%02d" i in
      match
        V.create ~db ~keys ~id
          ~now:(fixed_now +. float_of_int i)
          ~account:acct ~tokens:(sample_tokens ~n:i) ~scopes:[ "repo" ]
          ~expires_at:"2026-12-01T00:00:00Z" ()
      with
      | Error d -> fail_vdenial d
      | Ok r -> go (i + 1) (r :: acc)
  in
  go 1 []

let read_with ~db ~keys ~id =
  match V.read ~db ~keys ~id () with Ok o -> o | Error d -> fail_vdenial d

(* -------------------------------------------------------------------------- *)
(* Happy path rotation                                                        *)
(* -------------------------------------------------------------------------- *)

let test_happy_rotation () =
  with_db @@ fun db ->
  let seeds = seed_under_v1 ~db ~n:3 in
  let keys = rotation_keys () in
  let job =
    match
      R.start ~db ~keys ~id:"job_happy" ~now:fixed_now ~from_key_id:"mk-v1"
        ~from_key_version:1 ~to_key_id:"mk-v2" ~to_key_version:2 ()
    with
    | Ok j -> j
    | Error d -> fail_denial d
  in
  Alcotest.(check string) "phase" "in_progress" (R.string_of_phase job.phase);
  let batch =
    match
      R.rewrap_batch ~db ~keys ~job_id:job.id ~limit:10 ~now:fixed_now ()
    with
    | Ok b -> b
    | Error d -> fail_denial d
  in
  Alcotest.(check int) "rewrapped" 3 batch.rewrapped;
  Alcotest.(check int) "remaining" 0 batch.remaining_on_from;
  let verified =
    match R.verify_completion ~db ~keys ~job_id:job.id () with
    | Ok j -> j
    | Error d -> fail_denial d
  in
  Alcotest.(check string)
    "verified" "verified"
    (R.string_of_phase verified.phase);
  let completed =
    match R.complete_retire ~db ~keys ~job_id:job.id () with
    | Ok j -> j
    | Error d -> fail_denial d
  in
  Alcotest.(check string)
    "completed" "completed"
    (R.string_of_phase completed.phase);
  (* Tokens still open under dual keys; generation preserved. *)
  List.iteri
    (fun i (s : V.vault_record) ->
      let opened = read_with ~db ~keys ~id:s.id in
      Alcotest.(check string) "key_id" "mk-v2" opened.record.key_id;
      Alcotest.(check int) "key_version" 2 opened.record.key_version;
      Alcotest.(check int) "generation unchanged" 1 opened.record.generation;
      Alcotest.(check string)
        "access" (sample_tokens ~n:(i + 1)).access_token
        opened.tokens.access_token)
    seeds;
  (* Unique nonces: ciphertext changed after rewrap. *)
  let id0 = (List.hd seeds).id in
  match V.ciphertext_of ~db ~id:id0 with
  | Error d -> fail_vdenial d
  | Ok ct ->
      Alcotest.(check bool) "ciphertext non-empty" true (String.length ct > 20)

let test_unique_nonce_on_rewrap () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:1);
  let id = "ghvault_seed_01" in
  let ct_before =
    match V.ciphertext_of ~db ~id with Ok c -> c | Error d -> fail_vdenial d
  in
  let keys = rotation_keys () in
  (match
     R.start ~db ~keys ~from_key_id:"mk-v1" ~from_key_version:1
       ~to_key_id:"mk-v2" ~to_key_version:2 ()
   with
  | Ok _ -> ()
  | Error d -> fail_denial d);
  (match R.rewrap_batch ~db ~keys ~limit:1 () with
  | Ok _ -> ()
  | Error d -> fail_denial d);
  let ct_after =
    match V.ciphertext_of ~db ~id with Ok c -> c | Error d -> fail_vdenial d
  in
  Alcotest.(check bool)
    "fresh nonce changes ciphertext" true
    (not (String.equal ct_before ct_after))

(* -------------------------------------------------------------------------- *)
(* Crash resume                                                               *)
(* -------------------------------------------------------------------------- *)

let test_crash_resume () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:5);
  let keys = rotation_keys () in
  let job =
    match
      R.start ~db ~keys ~id:"job_resume" ~from_key_id:"mk-v1"
        ~from_key_version:1 ~to_key_id:"mk-v2" ~to_key_version:2 ()
    with
    | Ok j -> j
    | Error d -> fail_denial d
  in
  (* Partial rewrap (simulate crash mid-flight). *)
  let b1 =
    match R.rewrap_batch ~db ~keys ~job_id:job.id ~limit:2 () with
    | Ok b -> b
    | Error d -> fail_denial d
  in
  Alcotest.(check int) "partial rewrapped" 2 b1.rewrapped;
  Alcotest.(check int) "remaining after partial" 3 b1.remaining_on_from;
  (* "Restart": load_active and continue. *)
  let job2 =
    match R.load_active ~db with
    | Ok (Some j) -> j
    | Ok None -> Alcotest.fail "expected active job after crash"
    | Error d -> fail_denial d
  in
  Alcotest.(check string) "same job" job.id job2.id;
  let rec drain guard =
    if guard <= 0 then Alcotest.fail "resume drain budget"
    else
      match R.rewrap_batch ~db ~keys ~job_id:job2.id ~limit:2 () with
      | Error d -> fail_denial d
      | Ok b when b.remaining_on_from = 0 -> b
      | Ok _ -> drain (guard - 1)
  in
  let final = drain 10 in
  Alcotest.(check int) "all done" 0 final.remaining_on_from;
  match R.verify_completion ~db ~keys ~job_id:job2.id () with
  | Ok j ->
      Alcotest.(check string) "verified" "verified" (R.string_of_phase j.phase)
  | Error d -> fail_denial d

(* -------------------------------------------------------------------------- *)
(* Premature retire                                                           *)
(* -------------------------------------------------------------------------- *)

let test_reject_premature_retire () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:2);
  let keys = rotation_keys () in
  let job =
    match
      R.start ~db ~keys ~from_key_id:"mk-v1" ~from_key_version:1
        ~to_key_id:"mk-v2" ~to_key_version:2 ()
    with
    | Ok j -> j
    | Error d -> fail_denial d
  in
  (* No rewrap yet. *)
  (match R.complete_retire ~db ~keys ~job_id:job.id () with
  | Error (R.Premature_retire { remaining_on_from; _ }) ->
      Alcotest.(check int) "still on from" 2 remaining_on_from
  | Error d -> fail_denial d
  | Ok _ -> Alcotest.fail "premature retire should fail");
  ignore
    (match R.rewrap_batch ~db ~keys ~limit:1 () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  (match R.verify_completion ~db ~keys () with
  | Error (R.Premature_retire _) -> ()
  | Error d -> fail_denial d
  | Ok _ -> Alcotest.fail "verify with remaining should fail");
  (* Finish then retire succeeds. *)
  let rec drain g =
    if g <= 0 then ()
    else
      match R.rewrap_batch ~db ~keys ~limit:8 () with
      | Ok b when b.remaining_on_from = 0 -> ()
      | Ok _ -> drain (g - 1)
      | Error d -> fail_denial d
  in
  drain 5;
  ignore
    (match R.verify_completion ~db ~keys () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  match R.complete_retire ~db ~keys () with
  | Ok j ->
      Alcotest.(check string) "done" "completed" (R.string_of_phase j.phase)
  | Error d -> fail_denial d

(* -------------------------------------------------------------------------- *)
(* Rollback window                                                            *)
(* -------------------------------------------------------------------------- *)

let test_rollback_while_both_keys () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:3);
  let keys = rotation_keys () in
  ignore
    (match
       R.start ~db ~keys ~id:"job_rb" ~from_key_id:"mk-v1" ~from_key_version:1
         ~to_key_id:"mk-v2" ~to_key_version:2 ()
     with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  ignore
    (match R.rewrap_batch ~db ~keys ~limit:10 () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  ignore
    (match R.verify_completion ~db ~keys () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  (* Rollback while verified (both keys still authorized). *)
  let rb =
    match R.rollback_all ~db ~keys ~limit:8 () with
    | Ok b -> b
    | Error d -> fail_denial d
  in
  Alcotest.(check string)
    "rolled_back" "rolled_back"
    (R.string_of_phase rb.job.phase);
  (match V.count_for_key ~db ~key_id:"mk-v1" with
  | Ok n -> Alcotest.(check int) "restored to v1" 3 n
  | Error d -> fail_vdenial d);
  (match V.count_for_key ~db ~key_id:"mk-v2" with
  | Ok n -> Alcotest.(check int) "none on v2" 0 n
  | Error d -> fail_vdenial d);
  (* Tokens still readable under dual keys. *)
  let opened = read_with ~db ~keys ~id:"ghvault_seed_01" in
  Alcotest.(check string) "key back" "mk-v1" opened.record.key_id;
  Alcotest.(check string)
    "access intact" (sample_tokens ~n:1).access_token opened.tokens.access_token

let test_rollback_closed_after_retire () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:1);
  let keys = rotation_keys () in
  let job =
    match
      R.start ~db ~keys ~id:"job_retire_rb" ~from_key_id:"mk-v1"
        ~from_key_version:1 ~to_key_id:"mk-v2" ~to_key_version:2 ()
    with
    | Ok j -> j
    | Error d -> fail_denial d
  in
  ignore
    (match R.rewrap_batch ~db ~keys ~job_id:job.id ~limit:4 () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  ignore
    (match R.verify_completion ~db ~keys ~job_id:job.id () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  ignore
    (match R.complete_retire ~db ~keys ~job_id:job.id () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  (* Completed jobs leave load_active empty; address by id to prove the
     terminal phase rejects rollback. *)
  match R.rollback_batch ~db ~keys ~job_id:job.id () with
  | Error (R.Rollback_unavailable _) -> ()
  | Error d -> fail_denial d
  | Ok _ -> Alcotest.fail "rollback after retire must fail"

let test_rollback_requires_both_keys () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:1);
  let keys = rotation_keys () in
  ignore
    (match
       R.start ~db ~keys ~from_key_id:"mk-v1" ~from_key_version:1
         ~to_key_id:"mk-v2" ~to_key_version:2 ()
     with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  ignore
    (match R.rewrap_batch ~db ~keys ~limit:4 () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  (* Drop old key material — rollback must refuse. *)
  let only_new = single_keys ~key_id:"mk-v2" ~key_version:2 ~aes:aes_v2 in
  match R.rollback_batch ~db ~keys:only_new () with
  | Error (R.Rollback_unavailable _) | Error (R.Vault (V.Missing_key _)) -> ()
  | Error d -> fail_denial d
  | Ok _ -> Alcotest.fail "rollback without both keys must fail"

(* -------------------------------------------------------------------------- *)
(* Concurrent create during rewrap                                            *)
(* -------------------------------------------------------------------------- *)

let test_concurrent_create_during_rewrap () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:3);
  let keys = rotation_keys () in
  ignore
    (match
       R.start ~db ~keys ~from_key_id:"mk-v1" ~from_key_version:1
         ~to_key_id:"mk-v2" ~to_key_version:2 ()
     with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  (* Partial rewrap. *)
  ignore
    (match R.rewrap_batch ~db ~keys ~limit:1 () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  (* Concurrent create under active (v2) key mid-flight. *)
  let new_acct =
    account ~principal_id:"prin_concurrent" ~github_user_id:9999L ()
  in
  let created =
    match
      V.create ~db ~keys ~id:"ghvault_concurrent" ~account:new_acct
        ~tokens:
          {
            access_token = "ghu_concurrent_PLAIN";
            refresh_token = Some "ghr_concurrent_PLAIN";
          }
        ~scopes:[ "repo" ] ~expires_at:"2026-12-01T00:00:00Z" ()
    with
    | Ok r -> r
    | Error d -> fail_vdenial d
  in
  Alcotest.(check string) "new under active" "mk-v2" created.key_id;
  Alcotest.(check int) "gen 1" 1 created.generation;
  (* Finish rewrap; concurrent row must not be lost or re-keyed wrongly. *)
  let rec drain g =
    if g <= 0 then Alcotest.fail "drain"
    else
      match R.rewrap_batch ~db ~keys ~limit:8 () with
      | Ok b when b.remaining_on_from = 0 -> ()
      | Ok _ -> drain (g - 1)
      | Error d -> fail_denial d
  in
  drain 5;
  ignore
    (match R.verify_completion ~db ~keys () with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  let opened = read_with ~db ~keys ~id:created.id in
  Alcotest.(check string)
    "concurrent access intact" "ghu_concurrent_PLAIN" opened.tokens.access_token;
  Alcotest.(check string) "still v2" "mk-v2" opened.record.key_id;
  (* Seed rows also under v2 with intact tokens. *)
  let s1 = read_with ~db ~keys ~id:"ghvault_seed_01" in
  Alcotest.(check string)
    "seed access" (sample_tokens ~n:1).access_token s1.tokens.access_token

let test_concurrent_replace_preserves_authority () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:1);
  let keys = rotation_keys () in
  ignore
    (match
       R.start ~db ~keys ~from_key_id:"mk-v1" ~from_key_version:1
         ~to_key_id:"mk-v2" ~to_key_version:2 ()
     with
    | Ok _ -> ()
    | Error d -> fail_denial d);
  (* Concurrent replace advances generation under active key (effectively
     rewraps + rotates token lineage). *)
  let rotated =
    {
      S.access_token = "ghu_replaced_during_rewrap";
      refresh_token = Some "ghr_replaced_during_rewrap";
    }
  in
  (match
     V.replace ~db ~keys ~id:"ghvault_seed_01" ~expected_generation:1
       ~tokens:rotated ~scopes:[ "repo" ] ~expires_at:"2027-01-01T00:00:00Z" ()
   with
  | Ok r ->
      Alcotest.(check int) "gen advanced" 2 r.generation;
      Alcotest.(check string) "under active" "mk-v2" r.key_id
  | Error d -> fail_vdenial d);
  (* Rewrap should leave it alone (already on to_key). *)
  (match R.rewrap_batch ~db ~keys ~limit:4 () with
  | Ok b -> Alcotest.(check int) "remaining from" 0 b.remaining_on_from
  | Error d -> fail_denial d);
  let opened = read_with ~db ~keys ~id:"ghvault_seed_01" in
  Alcotest.(check string)
    "new token authority" "ghu_replaced_during_rewrap"
    opened.tokens.access_token;
  Alcotest.(check int) "gen kept" 2 opened.record.generation;
  (* Destroy does not revive authority. *)
  (match V.destroy ~db ~id:"ghvault_seed_01" with
  | Ok () -> ()
  | Error d -> fail_vdenial d);
  match V.read ~db ~keys ~id:"ghvault_seed_01" () with
  | Error V.Not_found -> ()
  | Error d -> fail_vdenial d
  | Ok _ -> Alcotest.fail "destroyed record must not reactivate"

(* -------------------------------------------------------------------------- *)
(* Mixed / unknown key fail-closed                                            *)
(* -------------------------------------------------------------------------- *)

let test_reject_mixed_unknown_keys () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:1);
  (* Plant a row under a third key. *)
  let k3 = single_keys ~key_id:"mk-v3" ~key_version:3 ~aes:aes_v3 in
  ignore
    (match
       V.create ~db ~keys:k3 ~id:"ghvault_foreign"
         ~account:(account ~principal_id:"prin_foreign" ~github_user_id:42L ())
         ~tokens:(sample_tokens ~n:99) ~scopes:[] ~expires_at:"t" ()
     with
    | Ok _ -> ()
    | Error d -> fail_vdenial d);
  let keys = rotation_keys () in
  match
    R.start ~db ~keys ~from_key_id:"mk-v1" ~from_key_version:1
      ~to_key_id:"mk-v2" ~to_key_version:2 ()
  with
  | Error (R.Unknown_or_mixed_key _) -> ()
  | Error d -> fail_denial d
  | Ok _ -> Alcotest.fail "mixed key vault must refuse start"

let test_start_requires_active_to_key () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:1);
  (* Active still on v1 — cannot start rotation to v2. *)
  let keys =
    rotation_keys ~active:"mk-v1" ~active_ver:1 ~active_aes:aes_v1
      ~from_id:"mk-v2" ~from_ver:2 ~from_aes:aes_v2 ~from_role:MK.Staged ()
  in
  match
    R.start ~db ~keys ~from_key_id:"mk-v1" ~from_key_version:1
      ~to_key_id:"mk-v2" ~to_key_version:2 ()
  with
  | Error (R.Active_key_mismatch _) -> ()
  | Error d -> fail_denial d
  | Ok _ -> Alcotest.fail "start requires active=to_key"

let test_job_json_no_secrets () =
  with_db @@ fun db ->
  ignore (seed_under_v1 ~db ~n:1);
  let keys = rotation_keys () in
  let job =
    match
      R.start ~db ~keys ~from_key_id:"mk-v1" ~from_key_version:1
        ~to_key_id:"mk-v2" ~to_key_version:2 ()
    with
    | Ok j -> j
    | Error d -> fail_denial d
  in
  let json = R.job_to_json job in
  Alcotest.(check bool)
    "no aes v1" false
    (V.json_contains_plaintext ~json ~plaintext:aes_v1);
  Alcotest.(check bool)
    "no aes v2" false
    (V.json_contains_plaintext ~json ~plaintext:aes_v2);
  Alcotest.(check bool)
    "no token" false
    (V.json_contains_plaintext ~json
       ~plaintext:(sample_tokens ~n:1).access_token)

let suite =
  [
    Alcotest.test_case "happy rotation rewrap verify retire" `Quick
      test_happy_rotation;
    Alcotest.test_case "rewrap uses unique AEAD nonces" `Quick
      test_unique_nonce_on_rewrap;
    Alcotest.test_case "crash resume continues rewrap" `Quick test_crash_resume;
    Alcotest.test_case "reject premature retire" `Quick
      test_reject_premature_retire;
    Alcotest.test_case "rollback while both keys authorized" `Quick
      test_rollback_while_both_keys;
    Alcotest.test_case "rollback closed after retire" `Quick
      test_rollback_closed_after_retire;
    Alcotest.test_case "rollback requires both keys" `Quick
      test_rollback_requires_both_keys;
    Alcotest.test_case "concurrent create during rewrap" `Quick
      test_concurrent_create_during_rewrap;
    Alcotest.test_case "concurrent replace preserves authority" `Quick
      test_concurrent_replace_preserves_authority;
    Alcotest.test_case "reject mixed/unknown key versions" `Quick
      test_reject_mixed_unknown_keys;
    Alcotest.test_case "start requires active to_key" `Quick
      test_start_requires_active_to_key;
    Alcotest.test_case "job json has no secrets" `Quick test_job_json_no_secrets;
  ]

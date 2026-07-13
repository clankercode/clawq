(** Cross-cutting vault security proofs (P21.M2.E4.T005).

    Covers restart, corruption, ciphertext/row swap, record replay, concurrent
    stale CAS write, key loss, destruction (no token-bearing secondary state),
    rotation crash / rewrap, backup/restore fail-closed paths, and the explicit
    whole-store rollback limitation without an external monotonic anchor.

    Security boundary under test (also documented in
    docs/github-vault-recovery.md):
    - Generation CAS + record AEAD detect live stale writes and record swaps.
    - An internally consistent whole-store rollback under the same available key
      is {b undetectable} without an external monotonic anchor; operator restore
      controls and compromise/revocation are the V1 mitigations. *)

module V = Github_user_token_vault
module S = Github_user_token_store
module MK = Github_user_token_master_key
module L = Github_user_token_lease
module C = Github_user_token_cas
module R = Github_user_token_rewrap
module Rec = Github_user_token_vault_recovery
module B = Github_account_binding
module P = Principal_identity
module PS = Principal_identity_store

let () = Secret_store.test_iterations_override := Some 1

let aes_v1 =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-sec-v1" ()

let aes_v2 =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-sec-v2" ()

let aes_wrong =
  Secret_store.derive_key ~iterations:1 ~passphrase:"gh-vault-sec-WRONG" ()

let tokens ?(tag = "base") () =
  {
    S.access_token = Printf.sprintf "ghu_access_SEC_%s_PLAIN" tag;
    refresh_token = Some (Printf.sprintf "ghr_refresh_SEC_%s_PLAIN" tag);
  }

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e
let fail_v d = Alcotest.fail (V.string_of_denial d)
let fail_c d = Alcotest.fail (C.string_of_denial d)
let fail_r d = Alcotest.fail (R.string_of_denial d)
let fail_rec d = Alcotest.fail (Rec.string_of_denial d)
let fail_l d = Alcotest.fail (L.string_of_denial d)

let account ?(principal_id = "prin_sec_1") ?(github_user_id = 8801L)
    ?(app_id = 61) () =
  assert_ok (V.make_account_key ~principal_id ~github_user_id ~app_id ())

let single ~key_id ~key_version ~aes =
  assert_ok (V.make_single_key_provider ~key_id ~key_version ~aes_key:aes ())

let rotation_keys () =
  let active_meta : MK.key_metadata =
    {
      key_id = "mk-sec-v2";
      key_version = 2;
      role = MK.Active;
      source_kind = MK.Env { var_name = "CLAWQ_GITHUB_VAULT_MASTER_KEY" };
    }
  in
  let from_meta : MK.key_metadata =
    {
      key_id = "mk-sec-v1";
      key_version = 1;
      role = MK.Backup_required;
      source_kind = MK.Env { var_name = "CLAWQ_GITHUB_VAULT_MASTER_KEY_OLD" };
    }
  in
  V.make_static_key_provider
    ~readiness:(MK.Ready { active = active_meta; available = [ from_meta ] })
    ~keys:
      [
        { V.key_id = "mk-sec-v2"; key_version = 2; aes_key = aes_v2 };
        { V.key_id = "mk-sec-v1"; key_version = 1; aes_key = aes_v1 };
      ]
    ()

let fixed_now = 1_720_300_000.0
let far_expires = "2026-12-01T00:00:00Z"

let with_mem_db f =
  let db = Sqlite3.db_open ":memory:" in
  Rec.ensure_schema db;
  R.ensure_schema db;
  B.ensure_schema db;
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      ignore (Sqlite3.db_close db))
    (fun () -> f db)

let with_file_db f =
  let path = Filename.temp_file "clawq_vault_sec_" ".db" in
  Fun.protect
    ~finally:(fun () ->
      ignore (L.discard_all ());
      (try Sys.remove path with Sys_error _ -> ());
      try Sys.remove (path ^ "-wal")
      with Sys_error _ -> (
        ();
        try Sys.remove (path ^ "-shm") with Sys_error _ -> ()))
    (fun () -> f path)

let create ~db ~keys ?(account = account ()) ?(toks = tokens ())
    ?(id = "ghvault_sec_1") () =
  match
    V.create ~db ~keys ~id ~now:fixed_now ~account ~tokens:toks
      ~scopes:[ "repo" ] ~expires_at:far_expires ()
  with
  | Ok r -> r
  | Error d -> fail_v d

let sql_text db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let buf = Buffer.create 256 in
      let rec go () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let cols = Sqlite3.data_count stmt in
            for i = 0 to cols - 1 do
              match Sqlite3.column stmt i with
              | Sqlite3.Data.TEXT s | Sqlite3.Data.BLOB s ->
                  Buffer.add_string buf s;
                  Buffer.add_char buf '\n'
              | _ -> ()
            done;
            go ()
        | _ -> ()
      in
      go ();
      Buffer.contents buf)

let table_exists db table =
  let sql =
    "SELECT 1 FROM sqlite_master WHERE type='table' AND name=? LIMIT 1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT table));
      match Sqlite3.step stmt with Sqlite3.Rc.ROW -> true | _ -> false)

let assert_no_plaintext ~db ~plaintexts =
  let tables =
    [
      "github_user_token_vault";
      "github_user_token_vault_recovery_state";
      "github_user_token_vault_recovery_events";
      "github_user_token_rewrap";
      "github_account_bindings";
      "github_account_binding_snapshots";
      "github_user_auth_tx";
    ]
  in
  let scan =
    List.fold_left
      (fun acc table ->
        if table_exists db table then
          acc ^ sql_text db (Printf.sprintf "SELECT * FROM %s" table)
        else acc)
      "" tables
  in
  List.iter
    (fun p ->
      if p <> "" && String_util.contains scan p then
        Alcotest.fail ("token-bearing secondary state leaked plaintext: " ^ p))
    plaintexts

let get_row_fields ~db ~id =
  let sql =
    {|SELECT ciphertext, generation, key_id, key_version, key_fingerprint,
             active, scopes_json, expires_at, principal_id, github_user_id,
             app_id, host, record_version, created_at, updated_at
      FROM github_user_token_vault WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let text i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.TEXT s -> s
            | Sqlite3.Data.BLOB s -> s
            | _ -> Alcotest.fail "expected text column"
          in
          let int i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.INT n -> Int64.to_int n
            | _ -> Alcotest.fail "expected int column"
          in
          let int64 i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.INT n -> n
            | Sqlite3.Data.TEXT s -> Int64.of_string s
            | _ -> Alcotest.fail "expected int64 column"
          in
          ( text 0,
            int 1,
            text 2,
            int 3,
            text 4,
            int 5,
            text 6,
            text 7,
            text 8,
            int64 9,
            int 10,
            text 11,
            int 12,
            text 13,
            text 14 )
      | _ -> Alcotest.fail ("missing vault row " ^ id))

let put_ciphertext_only ~db ~id ~ciphertext =
  let sql = "UPDATE github_user_token_vault SET ciphertext = ? WHERE id = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT ciphertext));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT id));
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let restore_full_row ~db ~id ~ct ~gen ~key_id ~key_version ~key_fp ~active
    ~scopes_json ~expires_at ~principal_id ~github_user_id ~app_id ~host
    ~record_version ~created_at ~updated_at =
  let sql =
    {|UPDATE github_user_token_vault SET
        ciphertext = ?, generation = ?, key_id = ?, key_version = ?,
        key_fingerprint = ?, active = ?, scopes_json = ?, expires_at = ?,
        principal_id = ?, github_user_id = ?, app_id = ?, host = ?,
        record_version = ?, created_at = ?, updated_at = ?
      WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i d = ignore (Sqlite3.bind stmt i d) in
  bind 1 (Sqlite3.Data.TEXT ct);
  bind 2 (Sqlite3.Data.INT (Int64.of_int gen));
  bind 3 (Sqlite3.Data.TEXT key_id);
  bind 4 (Sqlite3.Data.INT (Int64.of_int key_version));
  bind 5 (Sqlite3.Data.TEXT key_fp);
  bind 6 (Sqlite3.Data.INT (Int64.of_int active));
  bind 7 (Sqlite3.Data.TEXT scopes_json);
  bind 8 (Sqlite3.Data.TEXT expires_at);
  bind 9 (Sqlite3.Data.TEXT principal_id);
  bind 10 (Sqlite3.Data.INT github_user_id);
  bind 11 (Sqlite3.Data.INT (Int64.of_int app_id));
  bind 12 (Sqlite3.Data.TEXT host);
  bind 13 (Sqlite3.Data.INT (Int64.of_int record_version));
  bind 14 (Sqlite3.Data.TEXT created_at);
  bind 15 (Sqlite3.Data.TEXT updated_at);
  bind 16 (Sqlite3.Data.TEXT id);
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let copy_file ~src ~dst =
  let ic = open_in_bin src in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let oc = open_out_bin dst in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          let buf = Bytes.create 4096 in
          let rec go () =
            let n = input ic buf 0 (Bytes.length buf) in
            if n > 0 then (
              output oc buf 0 n;
              go ())
          in
          go ()))

(* -------------------------------------------------------------------------- *)
(* Security boundary constants                                                *)
(* -------------------------------------------------------------------------- *)

let test_boundary_constants () =
  Alcotest.(check bool)
    "whole-store rollback undetectable without external anchor" false
    Rec.whole_store_rollback_detectable_without_external_anchor;
  Alcotest.(check bool)
    "limitation statement mentions external monotonic anchor" true
    (String_util.contains Rec.whole_store_rollback_limitation_statement
       "external monotonic anchor");
  Alcotest.(check bool)
    "limitation tag non-empty" true
    (String.length Rec.whole_store_rollback_limitation_tag > 10)

(* -------------------------------------------------------------------------- *)
(* Restart: durable vault survives reopen; process-local leases do not        *)
(* -------------------------------------------------------------------------- *)

let test_restart_survives_reopen_and_drops_leases () =
  with_file_db @@ fun path ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let acct = account () in
  let toks = tokens ~tag:"restart" () in
  let id = "ghvault_sec_restart" in
  (* Phase 1: create + issue lease. *)
  let lease_handle =
    let db = Sqlite3.db_open path in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.db_close db))
      (fun () ->
        V.ensure_schema db;
        ignore (create ~db ~keys ~account:acct ~toks ~id ());
        match L.issue ~db ~now:fixed_now ~vault_id:id () with
        | Ok l -> L.handle_to_string (L.handle l)
        | Error d -> fail_l d)
  in
  (* Simulate process restart: clear process-local leases, reopen file. *)
  ignore (L.discard_all ());
  Alcotest.(check int) "leases cleared on restart" 0 (L.live_count ());
  let db2 = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db2))
    (fun () ->
      V.ensure_schema db2;
      (match V.read ~db:db2 ~keys ~id () with
      | Error d -> fail_v d
      | Ok opened ->
          Alcotest.(check string)
            "token survives restart" toks.access_token
            opened.tokens.access_token;
          Alcotest.(check int) "generation retained" 1 opened.record.generation);
      (* Pre-restart lease handle is not usable without re-issue. *)
      match L.handle_of_string lease_handle with
      | Error _ -> ()
      | Ok h -> (
          match L.revoke_handle ~handle:h with
          | false -> () (* not registered after restart — expected *)
          | true -> Alcotest.fail "pre-restart lease should not be registered"))

(* -------------------------------------------------------------------------- *)
(* Corruption                                                                 *)
(* -------------------------------------------------------------------------- *)

let test_corrupt_envelope_fail_closed () =
  with_mem_db @@ fun db ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let rec_ = create ~db ~keys ~id:"ghvault_sec_corrupt" () in
  put_ciphertext_only ~db ~id:rec_.id
    ~ciphertext:"$VAULT_AAD_V1:not-valid-base64!!!";
  match V.read ~db ~keys ~id:rec_.id () with
  | Error V.Corrupt_envelope -> ()
  | Error d -> fail_v d
  | Ok _ -> Alcotest.fail "corrupt envelope must not yield tokens"

(* -------------------------------------------------------------------------- *)
(* Ciphertext / row swap                                                      *)
(* -------------------------------------------------------------------------- *)

let test_ciphertext_row_swap_fail_closed () =
  with_mem_db @@ fun db ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let a1 = account ~principal_id:"prin_swap_a" ~github_user_id:1L () in
  let a2 = account ~principal_id:"prin_swap_b" ~github_user_id:2L () in
  let r1 =
    create ~db ~keys ~account:a1 ~id:"ghvault_swap_a"
      ~toks:(tokens ~tag:"swap_a" ()) ()
  in
  let r2 =
    create ~db ~keys ~account:a2 ~id:"ghvault_swap_b"
      ~toks:(tokens ~tag:"swap_b" ()) ()
  in
  let ct1, _, _, _, _, _, _, _, _, _, _, _, _, _, _ =
    get_row_fields ~db ~id:r1.id
  in
  let ct2, _, _, _, _, _, _, _, _, _, _, _, _, _, _ =
    get_row_fields ~db ~id:r2.id
  in
  put_ciphertext_only ~db ~id:r1.id ~ciphertext:ct2;
  put_ciphertext_only ~db ~id:r2.id ~ciphertext:ct1;
  (match V.read ~db ~keys ~id:r1.id () with
  | Error V.Swapped_record -> ()
  | Error d -> fail_v d
  | Ok { tokens; _ } ->
      ignore tokens;
      Alcotest.fail "swapped ciphertext must not open");
  match V.read ~db ~keys ~id:r2.id () with
  | Error V.Swapped_record -> ()
  | Error d -> fail_v d
  | Ok _ -> Alcotest.fail "swapped ciphertext must not open"

(* -------------------------------------------------------------------------- *)
(* Record replay: old ciphertext after generation advanced                    *)
(* -------------------------------------------------------------------------- *)

let test_record_replay_old_ciphertext_fail_closed () =
  with_mem_db @@ fun db ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let acct = account () in
  let old = tokens ~tag:"replay_old" () in
  let rec_ = create ~db ~keys ~account:acct ~toks:old ~id:"ghvault_replay" () in
  let old_ct, old_gen, _, _, _, _, _, _, _, _, _, _, _, _, _ =
    get_row_fields ~db ~id:rec_.id
  in
  Alcotest.(check int) "snapshot gen" 1 old_gen;
  let newer = tokens ~tag:"replay_new" () in
  (match
     C.replace ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ~tokens:newer ~scopes:[ "repo" ]
       ~expires_at:far_expires ()
   with
  | Ok t -> Alcotest.(check int) "gen advanced" 2 t.record.generation
  | Error d -> fail_c d);
  (* Replay only the old ciphertext onto the advanced generation row. AAD
     binds generation → fail closed as Swapped_record. *)
  put_ciphertext_only ~db ~id:rec_.id ~ciphertext:old_ct;
  match V.read ~db ~keys ~id:rec_.id () with
  | Error V.Swapped_record -> ()
  | Error d -> fail_v d
  | Ok opened ->
      ignore opened;
      Alcotest.fail "replayed old ciphertext must not yield tokens"

(* -------------------------------------------------------------------------- *)
(* Concurrent stale CAS write                                                 *)
(* -------------------------------------------------------------------------- *)

let test_concurrent_stale_cas_cannot_restore_old_token () =
  with_mem_db @@ fun db ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let acct = account () in
  let old = tokens ~tag:"stale_old" () in
  let rec_ = create ~db ~keys ~account:acct ~toks:old ~id:"ghvault_stale" () in
  let winner = tokens ~tag:"stale_winner" () in
  (match
     C.replace ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ~tokens:winner ~scopes:[ "repo" ]
       ~expires_at:far_expires ()
   with
  | Ok t -> Alcotest.(check int) "winner gen" 2 t.record.generation
  | Error d -> fail_c d);
  (match
     C.replace ~db ~keys ~now:(fixed_now +. 2.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ~tokens:old ~scopes:[]
       ~expires_at:far_expires ()
   with
  | Error (C.Vault (V.Generation_conflict { expected = 1; actual = 2 })) -> ()
  | Error d -> fail_c d
  | Ok _ -> Alcotest.fail "stale CAS must fail");
  match V.read ~db ~keys ~id:rec_.id () with
  | Error d -> fail_v d
  | Ok opened ->
      Alcotest.(check string)
        "winner retained" winner.access_token opened.tokens.access_token

(* -------------------------------------------------------------------------- *)
(* Key loss                                                                   *)
(* -------------------------------------------------------------------------- *)

let test_key_loss_fail_closed () =
  with_mem_db @@ fun db ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let rec_ = create ~db ~keys ~id:"ghvault_keyloss" () in
  let empty =
    V.make_static_key_provider
      ~readiness:
        (MK.Ready
           {
             active =
               {
                 key_id = "mk-other";
                 key_version = 9;
                 role = MK.Active;
                 source_kind = MK.Env { var_name = "X" };
               };
             available = [];
           })
      ~keys:[] ()
  in
  (match V.read ~db ~keys:empty ~id:rec_.id () with
  | Error (V.Missing_key { key_id = "mk-sec-v1" }) -> ()
  | Error d -> fail_v d
  | Ok _ -> Alcotest.fail "missing key must deny");
  let wrong = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_wrong in
  match V.read ~db ~keys:wrong ~id:rec_.id () with
  | Error V.Wrong_key -> ()
  | Error d -> fail_v d
  | Ok _ -> Alcotest.fail "wrong key material must deny"

(* -------------------------------------------------------------------------- *)
(* Destruction: no token-bearing secondary state                              *)
(* -------------------------------------------------------------------------- *)

let seed_binding ~db ~principal_id ~vault_id ~github_user_id ~app_id =
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
    B.make_binding ~id:"ghbind_sec_1" ~principal_id:pid ~identity
      ~authorization_status:B.Authorized ~vault_ref ~lineage_id:"lineage_sec" ()
  in
  assert_ok (B.insert ~db ~now:fixed_now b)

let test_destruction_leaves_no_token_secondary_state () =
  with_mem_db @@ fun db ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let acct =
    account ~principal_id:"prin_sec_dest" ~github_user_id:8801L ~app_id:61 ()
  in
  let toks = tokens ~tag:"destroy_me" () in
  let id = "ghvault_sec_destroy" in
  ignore (create ~db ~keys ~account:acct ~toks ~id ());
  let binding =
    seed_binding ~db ~principal_id:"prin_sec_dest" ~vault_id:id
      ~github_user_id:8801L ~app_id:61
  in
  let lease =
    match L.issue ~db ~now:fixed_now ~vault_id:id () with
    | Ok l -> l
    | Error d -> fail_l d
  in
  (* Unlink CAS: clear vault_ref, invalidate leases, deactivate. *)
  (match
     C.unlink ~db ~keys ~now:(fixed_now +. 1.) ~id ~expected_generation:1
       ~expected:acct ~binding_id:binding.id ()
   with
  | Ok t ->
      Alcotest.(check bool) "inactive" false t.record.active;
      Alcotest.(check bool) "lease invalidated" true (t.leases_invalidated >= 1)
  | Error d -> fail_c d);
  (* Physical destroy of sealed row. *)
  (match V.destroy ~db ~id with Ok () -> () | Error d -> fail_v d);
  ignore (L.discard_for_vault ~vault_id:id);
  (match V.read ~db ~keys ~id () with
  | Error V.Not_found -> ()
  | Error d -> fail_v d
  | Ok _ -> Alcotest.fail "destroyed row must Not_found");
  (match
     L.with_token ~db ~keys ~now:fixed_now ~lease
       ~f:(fun ~access_token:_ -> ())
       ()
   with
  | Error L.Lease_revoked
  | Error (L.Vault V.Not_found)
  | Error L.Lease_not_found ->
      ()
  | Error d -> fail_l d
  | Ok () -> Alcotest.fail "lease must not open after destroy");
  (match B.get ~db ~id:binding.id with
  | Error e -> Alcotest.fail e
  | Ok None -> Alcotest.fail "binding should remain for audit"
  | Ok (Some b) -> (
      match b.vault_ref with
      | None -> ()
      | Some _ -> Alcotest.fail "vault_ref must be cleared on unlink"));
  assert_no_plaintext ~db
    ~plaintexts:[ toks.access_token; Option.get toks.refresh_token; aes_v1 ];
  Alcotest.(check bool)
    "lease identity no token" false
    (L.identity_contains_plaintext ~identity:(L.identity_of lease)
       ~plaintext:toks.access_token)

(* -------------------------------------------------------------------------- *)
(* Rotation crash + rewrap                                                    *)
(* -------------------------------------------------------------------------- *)

let test_rotation_crash_resume_and_rewrap () =
  with_mem_db @@ fun db ->
  let k1 = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let rec seed i =
    if i > 4 then ()
    else
      let acct =
        account
          ~principal_id:(Printf.sprintf "prin_rot_%d" i)
          ~github_user_id:(Int64.of_int (9000 + i))
          ()
      in
      ignore
        (create ~db ~keys:k1 ~account:acct
           ~id:(Printf.sprintf "ghvault_rot_%02d" i)
           ~toks:(tokens ~tag:(Printf.sprintf "rot%d" i) ())
           ());
      seed (i + 1)
  in
  seed 1;
  let keys = rotation_keys () in
  let job =
    match
      R.start ~db ~keys ~id:"job_sec_crash" ~now:fixed_now
        ~from_key_id:"mk-sec-v1" ~from_key_version:1 ~to_key_id:"mk-sec-v2"
        ~to_key_version:2 ()
    with
    | Ok j -> j
    | Error d -> fail_r d
  in
  (* Partial rewrap then "crash". *)
  (match R.rewrap_batch ~db ~keys ~job_id:job.id ~limit:2 () with
  | Ok b ->
      Alcotest.(check int) "partial" 2 b.rewrapped;
      Alcotest.(check int) "remaining" 2 b.remaining_on_from
  | Error d -> fail_r d);
  let resumed =
    match R.load_active ~db with
    | Ok (Some j) -> j
    | Ok None -> Alcotest.fail "active job missing after crash"
    | Error d -> fail_r d
  in
  Alcotest.(check string) "same job" job.id resumed.id;
  let rec drain g =
    if g <= 0 then Alcotest.fail "drain budget"
    else
      match R.rewrap_batch ~db ~keys ~job_id:resumed.id ~limit:2 () with
      | Error d -> fail_r d
      | Ok b when b.remaining_on_from = 0 -> b
      | Ok _ -> drain (g - 1)
  in
  ignore (drain 8);
  (match R.verify_completion ~db ~keys ~job_id:resumed.id () with
  | Ok j ->
      Alcotest.(check string) "verified" "verified" (R.string_of_phase j.phase)
  | Error d -> fail_r d);
  match V.read ~db ~keys ~id:"ghvault_rot_01" () with
  | Error d -> fail_v d
  | Ok opened ->
      Alcotest.(check string) "under v2" "mk-sec-v2" opened.record.key_id;
      Alcotest.(check int) "gen preserved" 1 opened.record.generation;
      Alcotest.(check string)
        "token intact" (tokens ~tag:"rot1" ()).access_token
        opened.tokens.access_token

(* -------------------------------------------------------------------------- *)
(* Backup / restore fail closed                                               *)
(* -------------------------------------------------------------------------- *)

let restore_proof () =
  assert_ok
    (Rec.make_operator_proof ~operator_id:"ops_sec"
       ~approval:"APPROVE-SEC-RESTORE"
       ~acknowledged_limitations:[ Rec.whole_store_rollback_limitation_tag ]
       ())

let test_backup_restore_fail_closed_and_happy () =
  with_mem_db @@ fun db_src ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let toks = tokens ~tag:"backup" () in
  ignore
    (create ~db:db_src ~keys ~toks ~id:"ghvault_sec_bak"
       ~account:(account ~principal_id:"prin_bak" ~github_user_id:11L ())
       ());
  let backup = assert_ok (Rec.export_backup ~db:db_src ~now:fixed_now ()) in
  Alcotest.(check bool)
    "backup no access plaintext" false
    (Rec.backup_contains_plaintext ~backup ~plaintext:toks.access_token);
  Alcotest.(check bool)
    "backup no aes" false
    (Rec.backup_contains_plaintext ~backup ~plaintext:aes_v1);
  (* Wrong key material fails closed. *)
  let wrong = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_wrong in
  (match
     Rec.restore ~db:db_src ~keys:wrong ~proof:(restore_proof ()) ~backup ()
   with
  | Ok _ -> Alcotest.fail "wrong key restore must fail"
  | Error (Rec.Compatibility issues) ->
      Alcotest.(check bool)
        "unopenable reported" true
        (List.exists
           (function Rec.Unopenable_envelope _ -> true | _ -> false)
           issues)
  | Error d -> fail_rec d);
  (* Missing operator proof fails closed. *)
  (match
     Rec.restore ~db:db_src ~keys
       ~proof:
         {
           operator_id = "ops";
           approval = "";
           acknowledged_limitations =
             [ Rec.whole_store_rollback_limitation_tag ];
         }
       ~backup ()
   with
  | Error (Rec.Operator_proof_required _) -> ()
  | Error d -> fail_rec d
  | Ok _ -> Alcotest.fail "empty approval must fail");
  with_mem_db @@ fun db_dst ->
  let result =
    match
      Rec.restore ~db:db_dst ~keys ~proof:(restore_proof ()) ~backup
        ~now:fixed_now ()
    with
    | Ok r -> r
    | Error d -> fail_rec d
  in
  Alcotest.(check int) "imported" 1 result.imported;
  Alcotest.(check bool) "auth disabled" true result.authorization_disabled;
  match V.read ~db:db_dst ~keys ~id:"ghvault_sec_bak" () with
  | Error d -> fail_v d
  | Ok opened ->
      Alcotest.(check string)
        "restored access" toks.access_token opened.tokens.access_token

(* -------------------------------------------------------------------------- *)
(* Whole-store rollback under same key is undetectable without external anchor *)
(* -------------------------------------------------------------------------- *)

let test_whole_store_rollback_undetectable_under_same_key () =
  with_file_db @@ fun path ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let acct = account ~principal_id:"prin_rollback" ~github_user_id:42L () in
  let old = tokens ~tag:"snapshot_old" () in
  let id = "ghvault_sec_rollback" in
  let snapshot = path ^ ".snapshot" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove snapshot with Sys_error _ -> ())
    (fun () ->
      (* Create initial consistent store and snapshot the whole file. *)
      let db = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          ignore (Sqlite3.exec db "PRAGMA journal_mode=DELETE");
          V.ensure_schema db;
          ignore (create ~db ~keys ~account:acct ~toks:old ~id ()));
      copy_file ~src:path ~dst:snapshot;
      (* Advance generation with new tokens (live newer authority). *)
      let db2 = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db2))
        (fun () ->
          V.ensure_schema db2;
          let newer = tokens ~tag:"live_newer" () in
          (match
             C.replace ~db:db2 ~keys ~now:(fixed_now +. 5.) ~id
               ~expected_generation:1 ~expected:acct ~tokens:newer
               ~scopes:[ "repo" ] ~expires_at:far_expires ()
           with
          | Ok t -> Alcotest.(check int) "live gen" 2 t.record.generation
          | Error d -> fail_c d);
          match V.read ~db:db2 ~keys ~id () with
          | Ok opened ->
              Alcotest.(check string)
                "live newer" newer.access_token opened.tokens.access_token
          | Error d -> fail_v d);
      (* Attacker replaces whole DB with consistent older snapshot under the
         same key. V1 cannot detect this without an external monotonic anchor. *)
      copy_file ~src:snapshot ~dst:path;
      let db3 = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db3))
        (fun () ->
          V.ensure_schema db3;
          match V.read ~db:db3 ~keys ~id () with
          | Error d ->
              Alcotest.fail
                ("whole-store rollback under same key should open silently; \
                  got " ^ V.string_of_denial d)
          | Ok opened ->
              Alcotest.(check string)
                "old snapshot tokens reappear" old.access_token
                opened.tokens.access_token;
              Alcotest.(check int) "old generation" 1 opened.record.generation;
              Alcotest.(check bool)
                "constant documents undetectability" false
                Rec.whole_store_rollback_detectable_without_external_anchor);
      (* Operator restore path still requires proof and disables authorization
         rather than silently re-enabling act-as-user. *)
      let db4 = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db4))
        (fun () ->
          Rec.ensure_schema db4;
          let backup = assert_ok (Rec.export_backup ~db:db4 ()) in
          match
            Rec.restore ~db:db4 ~keys ~proof:(restore_proof ()) ~backup
              ~now:fixed_now ()
          with
          | Error d -> fail_rec d
          | Ok r ->
              Alcotest.(check bool)
                "restore disables auth (mitigation)" true
                r.authorization_disabled;
              Alcotest.(check bool)
                "gate off" false
                (assert_ok (Rec.user_authorization_enabled ~db:db4))))

(* -------------------------------------------------------------------------- *)
(* Consistent single-row rollback also opens (same class of limitation)       *)
(* -------------------------------------------------------------------------- *)

let test_consistent_row_restore_opens_under_same_key () =
  with_mem_db @@ fun db ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let acct = account () in
  let old = tokens ~tag:"row_old" () in
  let rec_ = create ~db ~keys ~account:acct ~toks:old ~id:"ghvault_row_rb" () in
  let ( ct,
        gen,
        key_id,
        key_version,
        key_fp,
        active,
        scopes_json,
        expires_at,
        principal_id,
        github_user_id,
        app_id,
        host,
        record_version,
        created_at,
        updated_at ) =
    get_row_fields ~db ~id:rec_.id
  in
  (match
     C.replace ~db ~keys ~now:(fixed_now +. 1.) ~id:rec_.id
       ~expected_generation:1 ~expected:acct ~tokens:(tokens ~tag:"row_new" ())
       ~scopes:[ "repo" ] ~expires_at:far_expires ()
   with
  | Ok t -> Alcotest.(check int) "advanced" 2 t.record.generation
  | Error d -> fail_c d);
  (* Full consistent row restore (ciphertext + generation + binding). Opens. *)
  restore_full_row ~db ~id:rec_.id ~ct ~gen ~key_id ~key_version ~key_fp ~active
    ~scopes_json ~expires_at ~principal_id ~github_user_id ~app_id ~host
    ~record_version ~created_at ~updated_at;
  match V.read ~db ~keys ~id:rec_.id () with
  | Error d -> fail_v d
  | Ok opened ->
      Alcotest.(check string)
        "consistent row rollback opens" old.access_token
        opened.tokens.access_token;
      Alcotest.(check int) "gen rolled back" 1 opened.record.generation

(* -------------------------------------------------------------------------- *)
(* Compromise path after key loss / destruction                               *)
(* -------------------------------------------------------------------------- *)

let test_compromise_disable_after_key_loss () =
  with_mem_db @@ fun db ->
  let keys = single ~key_id:"mk-sec-v1" ~key_version:1 ~aes:aes_v1 in
  let toks = tokens ~tag:"compromise" () in
  ignore (create ~db ~keys ~toks ~id:"ghvault_comp" ());
  let proof =
    assert_ok
      (Rec.make_operator_proof ~operator_id:"ops_comp"
         ~approval:"APPROVE-COMPROMISE"
         ~acknowledged_limitations:[ Rec.compromise_relink_required_tag ]
         ())
  in
  let result =
    match
      Rec.compromise_disable ~db ~proof
        ~reason:"suspected master key loss from offline media" ~now:fixed_now ()
    with
    | Ok r -> r
    | Error d -> fail_rec d
  in
  Alcotest.(check int) "vault destroyed" 1 result.vault_records_destroyed;
  Alcotest.(check bool) "requires relink" true result.requires_relink;
  Alcotest.(check bool) "requires rotation" true result.requires_key_rotation;
  (match V.count_all ~db with
  | Ok 0 -> ()
  | Ok n -> Alcotest.fail (Printf.sprintf "expected empty vault, got %d" n)
  | Error d -> fail_v d);
  assert_no_plaintext ~db
    ~plaintexts:[ toks.access_token; Option.get toks.refresh_token ]

let suite =
  [
    Alcotest.test_case "security boundary constants" `Quick
      test_boundary_constants;
    Alcotest.test_case "restart: durable vault + lease drop" `Quick
      test_restart_survives_reopen_and_drops_leases;
    Alcotest.test_case "corruption fails closed" `Quick
      test_corrupt_envelope_fail_closed;
    Alcotest.test_case "ciphertext/row swap fails closed" `Quick
      test_ciphertext_row_swap_fail_closed;
    Alcotest.test_case "record replay of old ciphertext fails closed" `Quick
      test_record_replay_old_ciphertext_fail_closed;
    Alcotest.test_case "concurrent stale CAS cannot restore old token" `Quick
      test_concurrent_stale_cas_cannot_restore_old_token;
    Alcotest.test_case "key loss / wrong key fail closed" `Quick
      test_key_loss_fail_closed;
    Alcotest.test_case "destruction leaves no token-bearing secondary state"
      `Quick test_destruction_leaves_no_token_secondary_state;
    Alcotest.test_case "rotation crash resume and rewrap" `Quick
      test_rotation_crash_resume_and_rewrap;
    Alcotest.test_case "backup/restore fail closed + happy path" `Quick
      test_backup_restore_fail_closed_and_happy;
    Alcotest.test_case
      "whole-store rollback under same key undetectable without external anchor"
      `Quick test_whole_store_rollback_undetectable_under_same_key;
    Alcotest.test_case "consistent single-row restore opens under same key"
      `Quick test_consistent_row_restore_opens_under_same_key;
    Alcotest.test_case "compromise disable after key loss" `Quick
      test_compromise_disable_after_key_loss;
  ]
